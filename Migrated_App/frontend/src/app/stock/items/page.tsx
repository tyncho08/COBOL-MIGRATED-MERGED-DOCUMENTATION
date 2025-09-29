'use client'

import { useState, useEffect } from 'react'
import { 
  CubeIcon,
  MagnifyingGlassIcon,
  PlusIcon,
  DocumentArrowDownIcon,
  DocumentArrowUpIcon,
  ArrowsRightLeftIcon,
  ChartBarIcon,
  ExclamationTriangleIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import Table from '@/components/UI/Table'
import PageHeader from '@/components/Layout/PageHeader'
import Modal from '@/components/UI/Modal'
import { formatCurrency } from '@/lib/utils'

interface StockItem {
  stock_key: string
  stock_desc: string
  stock_qty_on_hand: number
  stock_avg_cost: number
  stock_location: string
  stock_product_group: string
  stock_reorder_point: number
  stock_reorder_qty: number
  stock_unit: string
  total_value: number
  status: 'normal' | 'low' | 'critical' | 'overstocked'
}

export default function StockItemsPage() {
  const [items, setItems] = useState<StockItem[]>([])
  const [loading, setLoading] = useState(true)
  const [searchTerm, setSearchTerm] = useState('')
  const [selectedItems, setSelectedItems] = useState<Set<string>>(new Set())
  const [showMovementModal, setShowMovementModal] = useState(false)
  const [movementType, setMovementType] = useState<'receipt' | 'issue' | 'transfer' | 'adjustment'>('receipt')

  useEffect(() => {
    fetchStockItems()
  }, [])

  const fetchStockItems = async () => {
    try {
      setLoading(true)
      
      // Fetch from COBOL endpoint
      const response = await fetch('http://localhost:8000/api/v1/stock/cobol/items')
      if (response.ok) {
        const data = await response.json()
        // Process items to add calculated fields
        const processedItems = data.items.map((item: any) => {
          const qty = item.quantity || 0
          const reorderPoint = item.reorder_point || 0
          let status: StockItem['status'] = 'normal'
          
          if (qty <= 0) {
            status = 'critical'
          } else if (qty <= reorderPoint) {
            status = 'low'
          } else if (qty > reorderPoint * 3) {
            status = 'overstocked'
          }
          
          return {
            stock_key: item.item_code,
            stock_desc: item.description,
            stock_qty_on_hand: qty,
            stock_avg_cost: item.unit_cost,
            stock_location: item.location || 'MAIN',
            stock_product_group: item.product_group || 'GENERAL',
            stock_reorder_point: reorderPoint,
            stock_reorder_qty: item.reorder_qty,
            stock_unit: item.unit || 'EA',
            total_value: item.total_value,
            status
          }
        })
        setItems(processedItems)
      } else {
        console.error('Failed to fetch stock items:', response.status)
      }
    } catch (error) {
      console.error('Failed to fetch stock items:', error)
    } finally {
      setLoading(false)
    }
  }

  // Filter items based on search
  const filteredItems = items.filter(item =>
    item.stock_key.toLowerCase().includes(searchTerm.toLowerCase()) ||
    item.stock_desc.toLowerCase().includes(searchTerm.toLowerCase()) ||
    item.stock_product_group.toLowerCase().includes(searchTerm.toLowerCase())
  )

  const handleMovement = (type: typeof movementType) => {
    setMovementType(type)
    setShowMovementModal(true)
  }

  const getStatusBadge = (status: StockItem['status'], qty: number) => {
    switch (status) {
      case 'critical':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            <ExclamationTriangleIcon className="w-3 h-3 mr-1" />
            {qty === 0 ? 'Out of Stock' : 'Critical'}
          </span>
        )
      case 'low':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
            Low Stock
          </span>
        )
      case 'overstocked':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
            Overstocked
          </span>
        )
      default:
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
            Normal
          </span>
        )
    }
  }

  const columns = [
    {
      key: 'stock_key',
      header: 'Item Code',
      render: (value: any, row: StockItem) => (
        <div className="font-medium text-gray-900">{row.stock_key}</div>
      )
    },
    {
      key: 'stock_desc',
      header: 'Description',
      render: (value: any, row: StockItem) => (
        <div>
          <div className="font-medium text-gray-900">{row.stock_desc}</div>
          <div className="text-sm text-gray-500">{row.stock_product_group} - {row.stock_location}</div>
        </div>
      )
    },
    {
      key: 'stock_qty_on_hand',
      header: 'On Hand',
      render: (value: any, row: StockItem) => (
        <div className="text-right">
          <div className="font-medium">{row.stock_qty_on_hand} {row.stock_unit}</div>
          {row.stock_reorder_point > 0 && (
            <div className="text-sm text-gray-500">Reorder: {row.stock_reorder_point}</div>
          )}
        </div>
      )
    },
    {
      key: 'stock_avg_cost',
      header: 'Unit Cost',
      render: (value: any, row: StockItem) => (
        <div className="text-right">{formatCurrency(row.stock_avg_cost)}</div>
      )
    },
    {
      key: 'total_value',
      header: 'Total Value',
      render: (value: any, row: StockItem) => (
        <div className="text-right font-medium">{formatCurrency(row.total_value)}</div>
      )
    },
    {
      key: 'status',
      header: 'Status',
      render: (value: any, row: StockItem) => getStatusBadge(row.status, row.stock_qty_on_hand)
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (value: any, row: StockItem) => (
        <div className="flex space-x-2">
          <Button 
            variant="outline" 
            size="sm"
            onClick={() => alert(`View history for ${row.stock_key}`)}
          >
            History
          </Button>
          <Button 
            variant="outline" 
            size="sm"
            onClick={() => {
              setSelectedItems(new Set([row.stock_key]))
              setShowMovementModal(true)
            }}
          >
            Movement
          </Button>
        </div>
      )
    }
  ]

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm" onClick={() => handleMovement('receipt')}>
        <DocumentArrowDownIcon className="h-4 w-4" />
        Goods Receipt
      </Button>
      <Button variant="outline" size="sm" onClick={() => handleMovement('issue')}>
        <DocumentArrowUpIcon className="h-4 w-4" />
        Stock Issue
      </Button>
      <Button variant="outline" size="sm" onClick={() => handleMovement('transfer')}>
        <ArrowsRightLeftIcon className="h-4 w-4" />
        Transfer
      </Button>
      <Button size="sm">
        <PlusIcon className="h-4 w-4" />
        New Item
      </Button>
    </div>
  )

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Stock Items"
        description="Manage inventory items and stock levels"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Stock Control', href: '/stock' },
          { label: 'Items' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Search and Filters */}
        <Card className="mb-6">
          <div className="p-6">
            <div className="flex items-center space-x-4">
              <div className="flex-1">
                <Input
                  type="text"
                  placeholder="Search items by code, description, or category..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  leftIcon={<MagnifyingGlassIcon className="h-5 w-5" />}
                />
              </div>
              <Button variant="outline">
                <ChartBarIcon className="h-4 w-4" />
                Stock Report
              </Button>
            </div>
          </div>
        </Card>

        {/* Items Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <h3 className="text-lg font-medium text-gray-900">
                Stock Items
                {searchTerm && (
                  <span className="text-sm font-normal text-gray-500 ml-2">
                    ({filteredItems.length} of {items.length} items)
                  </span>
                )}
              </h3>
              <div className="text-sm text-gray-500">
                Total Value: {formatCurrency(items.reduce((sum, item) => sum + item.total_value, 0))}
              </div>
            </div>
          </div>
          <Table
            data={filteredItems}
            columns={columns}
            loading={loading}
            emptyMessage="No stock items found"
            selection={{
              selectedRows: selectedItems,
              onRowSelect: (index: number) => {
                const newSelected = new Set(selectedItems)
                const item = filteredItems[index]
                if (newSelected.has(item.stock_key)) {
                  newSelected.delete(item.stock_key)
                } else {
                  newSelected.add(item.stock_key)
                }
                setSelectedItems(newSelected)
              },
              onSelectAll: () => {
                if (selectedItems.size === filteredItems.length) {
                  setSelectedItems(new Set())
                } else {
                  setSelectedItems(new Set(filteredItems.map(item => item.stock_key)))
                }
              }
            }}
          />
        </Card>
      </main>

      {/* Movement Modal */}
      <Modal
        isOpen={showMovementModal}
        onClose={() => setShowMovementModal(false)}
        title={`Stock ${movementType.charAt(0).toUpperCase() + movementType.slice(1)}`}
      >
        <div className="p-6">
          <p className="text-gray-600 mb-4">
            Stock movement functionality will be implemented here.
          </p>
          <div className="flex justify-end space-x-3">
            <Button variant="outline" onClick={() => setShowMovementModal(false)}>
              Cancel
            </Button>
            <Button onClick={() => {
              alert(`Processing ${movementType}...`)
              setShowMovementModal(false)
            }}>
              Process
            </Button>
          </div>
        </div>
      </Modal>
    </div>
  )
}