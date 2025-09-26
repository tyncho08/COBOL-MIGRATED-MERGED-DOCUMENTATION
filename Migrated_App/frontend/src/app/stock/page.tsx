'use client'

import { useState, useEffect } from 'react'
import { 
  CubeIcon,
  TruckIcon,
  ClipboardDocumentListIcon,
  ChartBarIcon,
  CurrencyDollarIcon,
  ExclamationTriangleIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'

interface StockSummary {
  total_items: number
  total_value: number
  total_quantity: number
  low_stock_items: number
  negative_stock_items: number
  slow_moving_items: number
  categories_count: number
  locations_count: number
}

interface RecentMovement {
  id: number
  item_code: string
  description: string
  movement_type: string
  quantity: number
  location: string
  date: string
  reference: string
}

export default function StockControlPage() {
  const [summary, setSummary] = useState<StockSummary | null>(null)
  const [recentMovements, setRecentMovements] = useState<RecentMovement[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    const fetchData = async () => {
      try {
        // Simulate API calls
        await new Promise(resolve => setTimeout(resolve, 1000))
        
        setSummary({
          total_items: 432,
          total_value: 78450.50,
          total_quantity: 15678.00,
          low_stock_items: 23,
          negative_stock_items: 3,
          slow_moving_items: 45,
          categories_count: 12,
          locations_count: 8
        })

        setRecentMovements([
          {
            id: 1,
            item_code: 'ITM001',
            description: 'Widget Assembly A1',
            movement_type: 'RECEIPT',
            quantity: 100,
            location: 'WH-01',
            date: '2024-01-15T10:30:00Z',
            reference: 'PO-2024-001'
          },
          {
            id: 2,
            item_code: 'ITM002',
            description: 'Bolt Kit Standard',
            movement_type: 'ISSUE',
            quantity: -25,
            location: 'WH-01',
            date: '2024-01-15T09:15:00Z',
            reference: 'WO-2024-045'
          },
          {
            id: 3,
            item_code: 'ITM003',
            description: 'Circuit Board XL',
            movement_type: 'ADJUSTMENT',
            quantity: 5,
            location: 'WH-02',
            date: '2024-01-15T08:45:00Z',
            reference: 'ADJ-2024-012'
          }
        ])
      } catch (error) {
        console.error('Failed to fetch stock data:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchData()
  }, [])

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <ClipboardDocumentListIcon className="h-4 w-4" />
        Stock Take
      </Button>
      <Button variant="outline" size="sm">
        <TruckIcon className="h-4 w-4" />
        Receive Stock
      </Button>
      <Button size="sm">
        <CubeIcon className="h-4 w-4" />
        New Item
      </Button>
    </div>
  )

  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-GB', {
      style: 'currency',
      currency: 'GBP'
    }).format(amount)
  }

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('en-GB', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    })
  }

  const getMovementTypeColor = (type: string) => {
    switch (type) {
      case 'RECEIPT':
        return 'bg-green-100 text-green-800'
      case 'ISSUE':
        return 'bg-red-100 text-red-800'
      case 'ADJUSTMENT':
        return 'bg-yellow-100 text-yellow-800'
      case 'TRANSFER':
        return 'bg-blue-100 text-blue-800'
      default:
        return 'bg-gray-100 text-gray-800'
    }
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Stock Control"
        description="Inventory management with comprehensive tracking and valuation"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Stock Control' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        {summary && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            <StatsCard
              title="Total Items"
              value={summary.total_items.toLocaleString()}
              icon={<CubeIcon className="h-6 w-6" />}
              href="/stock/items"
            />
            <StatsCard
              title="Total Value"
              value={formatCurrency(summary.total_value)}
              icon={<CurrencyDollarIcon className="h-6 w-6" />}
              href="/stock/valuation"
            />
            <StatsCard
              title="Low Stock Alerts"
              value={summary.low_stock_items}
              icon={<ExclamationTriangleIcon className="h-6 w-6" />}
              change={{ value: `${summary.negative_stock_items} negative`, type: 'decrease' }}
              href="/stock/alerts"
            />
            <StatsCard
              title="Slow Moving"
              value={summary.slow_moving_items}
              icon={<ChartBarIcon className="h-6 w-6" />}
              href="/stock/analysis"
            />
          </div>
        )}

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Recent Movements */}
          <div className="lg:col-span-2">
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <div className="flex items-center justify-between">
                  <h3 className="text-lg font-medium text-gray-900">Recent Movements</h3>
                  <Button variant="outline" size="sm">
                    View All
                  </Button>
                </div>
              </div>
              <div className="p-0">
                {loading ? (
                  <div className="p-6">
                    <div className="animate-pulse space-y-4">
                      {Array.from({ length: 3 }).map((_, index) => (
                        <div key={index} className="flex items-center space-x-4">
                          <div className="h-10 w-10 bg-gray-300 rounded"></div>
                          <div className="flex-1 space-y-2">
                            <div className="h-4 bg-gray-300 rounded w-3/4"></div>
                            <div className="h-3 bg-gray-300 rounded w-1/2"></div>
                          </div>
                        </div>
                      ))}
                    </div>
                  </div>
                ) : (
                  <div className="divide-y divide-gray-200">
                    {recentMovements.map((movement) => (
                      <div key={movement.id} className="p-6 hover:bg-gray-50">
                        <div className="flex items-center justify-between">
                          <div className="flex items-center space-x-3">
                            <div className={`px-2 py-1 text-xs font-medium rounded-full ${getMovementTypeColor(movement.movement_type)}`}>
                              {movement.movement_type}
                            </div>
                            <div>
                              <p className="text-sm font-medium text-gray-900">
                                {movement.item_code} - {movement.description}
                              </p>
                              <p className="text-sm text-gray-500">
                                {movement.location} • {movement.reference}
                              </p>
                            </div>
                          </div>
                          <div className="text-right">
                            <p className={`text-sm font-medium ${movement.quantity >= 0 ? 'text-green-600' : 'text-red-600'}`}>
                              {movement.quantity >= 0 ? '+' : ''}{movement.quantity}
                            </p>
                            <p className="text-xs text-gray-500">
                              {formatDate(movement.date)}
                            </p>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            </Card>
          </div>

          {/* Quick Actions & Info */}
          <div className="space-y-6">
            {/* Quick Actions */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Quick Actions</h3>
              </div>
              <div className="p-6 space-y-3">
                <Button variant="outline" className="w-full justify-start">
                  <CubeIcon className="h-4 w-4 mr-2" />
                  Item Inquiry
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <TruckIcon className="h-4 w-4 mr-2" />
                  Goods Receipt
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <ClipboardDocumentListIcon className="h-4 w-4 mr-2" />
                  Stock Issue
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <ChartBarIcon className="h-4 w-4 mr-2" />
                  Stock Transfer
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <CurrencyDollarIcon className="h-4 w-4 mr-2" />
                  Valuation Report
                </Button>
              </div>
            </Card>

            {/* System Info */}
            {summary && (
              <Card>
                <div className="px-6 py-4 border-b border-gray-200">
                  <h3 className="text-lg font-medium text-gray-900">System Information</h3>
                </div>
                <div className="p-6">
                  <dl className="space-y-3 text-sm">
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Categories:</dt>
                      <dd className="font-medium text-gray-900">{summary.categories_count}</dd>
                    </div>
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Locations:</dt>
                      <dd className="font-medium text-gray-900">{summary.locations_count}</dd>
                    </div>
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Costing Method:</dt>
                      <dd className="font-medium text-gray-900">FIFO/LIFO/Avg</dd>
                    </div>
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Last Stock Take:</dt>
                      <dd className="font-medium text-gray-900">15/01/2024</dd>
                    </div>
                  </dl>
                </div>
              </Card>
            )}

            {/* Alerts */}
            {summary && summary.low_stock_items > 0 && (
              <Card>
                <div className="px-6 py-4 border-b border-gray-200">
                  <h3 className="text-lg font-medium text-gray-900">Alerts</h3>
                </div>
                <div className="p-6">
                  <div className="rounded-md bg-yellow-50 p-4">
                    <div className="flex">
                      <div className="flex-shrink-0">
                        <ExclamationTriangleIcon className="h-5 w-5 text-yellow-400" />
                      </div>
                      <div className="ml-3">
                        <h3 className="text-sm font-medium text-yellow-800">
                          Stock Attention Required
                        </h3>
                        <div className="mt-2 text-sm text-yellow-700">
                          <ul className="list-disc pl-5 space-y-1">
                            <li>{summary.low_stock_items} items below minimum stock</li>
                            {summary.negative_stock_items > 0 && (
                              <li>{summary.negative_stock_items} items with negative stock</li>
                            )}
                            <li>{summary.slow_moving_items} slow-moving items</li>
                          </ul>
                        </div>
                        <div className="mt-4">
                          <Button size="sm" variant="outline">
                            View Details
                          </Button>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </Card>
            )}
          </div>
        </div>
      </main>
    </div>
  )
}