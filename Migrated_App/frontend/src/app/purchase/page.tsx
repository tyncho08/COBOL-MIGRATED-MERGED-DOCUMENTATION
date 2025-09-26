'use client'

import { useState, useEffect } from 'react'
import { 
  TruckIcon,
  DocumentTextIcon,
  CurrencyDollarIcon,
  ClockIcon,
  ExclamationTriangleIcon,
  CheckCircleIcon,
  ShoppingCartIcon,
  BanknotesIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import { formatCurrency, formatDate } from '@/lib/utils'

interface PLSummary {
  total_suppliers: number
  active_suppliers: number
  total_payable: number
  overdue_payments: number
  current_month_purchases: number
  pending_orders: number
  pending_invoices: number
  pending_approvals: number
  average_payment_days: number
}

interface RecentPurchase {
  id: number
  po_number?: string
  invoice_number?: string
  supplier_code: string
  supplier_name: string
  amount: number
  due_date?: string
  order_date?: string
  status: 'ordered' | 'received' | 'invoiced' | 'paid' | 'pending_approval'
  days_outstanding?: number
  type: 'order' | 'invoice'
}

interface PaymentSchedule {
  supplier: string
  amount: number
  due_date: string
  days_until_due: number
  priority: 'high' | 'medium' | 'low'
}

export default function PurchaseLedgerPage() {
  const [summary, setSummary] = useState<PLSummary | null>(null)
  const [recentPurchases, setRecentPurchases] = useState<RecentPurchase[]>([])
  const [paymentSchedule, setPaymentSchedule] = useState<PaymentSchedule[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    const fetchData = async () => {
      try {
        // Simulate API calls
        await new Promise(resolve => setTimeout(resolve, 1000))
        
        setSummary({
          total_suppliers: 87,
          active_suppliers: 76,
          total_payable: 23150.75,
          overdue_payments: 4250.00,
          current_month_purchases: 45670.00,
          pending_orders: 12,
          pending_invoices: 8,
          pending_approvals: 5,
          average_payment_days: 28
        })

        setRecentPurchases([
          {
            id: 1,
            po_number: 'PO-2024-0089',
            supplier_code: 'SUP001',
            supplier_name: 'Office Supplies Ltd',
            amount: 1450.00,
            order_date: '2024-01-15',
            status: 'ordered',
            type: 'order'
          },
          {
            id: 2,
            invoice_number: 'INV-SUP002-456',
            supplier_code: 'SUP002',
            supplier_name: 'Tech Equipment Corp',
            amount: 2850.75,
            due_date: '2024-02-15',
            status: 'pending_approval',
            days_outstanding: 0,
            type: 'invoice'
          },
          {
            id: 3,
            invoice_number: 'INV-SUP003-789',
            supplier_code: 'SUP003',
            supplier_name: 'Industrial Materials Inc',
            amount: 1200.00,
            due_date: '2024-01-30',
            status: 'invoiced',
            days_outstanding: 15,
            type: 'invoice'
          }
        ])

        setPaymentSchedule([
          {
            supplier: 'Tech Equipment Corp',
            amount: 2850.75,
            due_date: '2024-02-15',
            days_until_due: 3,
            priority: 'high'
          },
          {
            supplier: 'Industrial Materials Inc',
            amount: 1200.00,
            due_date: '2024-02-18',
            days_until_due: 6,
            priority: 'medium'
          },
          {
            supplier: 'Office Supplies Ltd',
            amount: 650.50,
            due_date: '2024-02-25',
            days_until_due: 13,
            priority: 'low'
          }
        ])
      } catch (error) {
        console.error('Failed to fetch purchase ledger data:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchData()
  }, [])

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <ShoppingCartIcon className="h-4 w-4" />
        New PO
      </Button>
      <Button variant="outline" size="sm">
        <DocumentTextIcon className="h-4 w-4" />
        Enter Invoice
      </Button>
      <Button size="sm">
        <TruckIcon className="h-4 w-4" />
        New Supplier
      </Button>
    </div>
  )



  const getStatusColor = (status: string) => {
    switch (status) {
      case 'paid':
        return 'bg-green-100 text-green-800'
      case 'ordered':
        return 'bg-blue-100 text-blue-800'
      case 'received':
        return 'bg-purple-100 text-purple-800'
      case 'invoiced':
        return 'bg-yellow-100 text-yellow-800'
      case 'pending_approval':
        return 'bg-orange-100 text-orange-800'
      default:
        return 'bg-gray-100 text-gray-800'
    }
  }

  const getPriorityColor = (priority: string) => {
    switch (priority) {
      case 'high':
        return 'bg-red-100 text-red-800'
      case 'medium':
        return 'bg-yellow-100 text-yellow-800'
      case 'low':
        return 'bg-green-100 text-green-800'
      default:
        return 'bg-gray-100 text-gray-800'
    }
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Purchase Ledger"
        description="Supplier management, purchase orders, and accounts payable"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Purchase Ledger' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        {summary && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            <StatsCard
              title="Active Suppliers"
              value={summary.active_suppliers.toLocaleString()}
              icon={<TruckIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.total_suppliers} total`, 
                type: 'neutral' 
              }}
              href="/purchase/suppliers"
            />
            <StatsCard
              title="Total Payable"
              value={formatCurrency(summary.total_payable)}
              icon={<CurrencyDollarIcon className="h-6 w-6" />}
              change={{ 
                value: formatCurrency(summary.overdue_payments) + ' overdue', 
                type: summary.overdue_payments > 0 ? 'decrease' : 'neutral' 
              }}
              href="/purchase/payables"
            />
            <StatsCard
              title="This Month Purchases"
              value={formatCurrency(summary.current_month_purchases)}
              icon={<ShoppingCartIcon className="h-6 w-6" />}
              href="/purchase/reports"
            />
            <StatsCard
              title="Pending Items"
              value={summary.pending_orders + summary.pending_invoices + summary.pending_approvals}
              icon={<ClockIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.pending_approvals} need approval`, 
                type: summary.pending_approvals > 0 ? 'decrease' : 'neutral' 
              }}
              href="/purchase/pending"
            />
          </div>
        )}

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Recent Purchase Orders & Invoices */}
          <div className="lg:col-span-2">
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <div className="flex items-center justify-between">
                  <h3 className="text-lg font-medium text-gray-900">Recent Activity</h3>
                  <div className="flex space-x-2">
                    <Button variant="outline" size="sm">
                      Orders
                    </Button>
                    <Button variant="outline" size="sm">
                      Invoices
                    </Button>
                  </div>
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
                    {recentPurchases.map((purchase) => (
                      <div key={purchase.id} className="p-6 hover:bg-gray-50">
                        <div className="flex items-center justify-between">
                          <div className="flex items-center space-x-3">
                            <div className={`px-2 py-1 text-xs font-medium rounded-full ${getStatusColor(purchase.status)}`}>
                              {purchase.status.replace('_', ' ').toUpperCase()}
                            </div>
                            <div>
                              <p className="text-sm font-medium text-gray-900">
                                {purchase.po_number || purchase.invoice_number}
                              </p>
                              <p className="text-sm text-gray-500">
                                {purchase.supplier_code} - {purchase.supplier_name}
                              </p>
                              <div className="flex items-center space-x-2 text-xs text-gray-400">
                                {purchase.order_date && (
                                  <span>Ordered: {formatDate(purchase.order_date)}</span>
                                )}
                                {purchase.due_date && (
                                  <span>Due: {formatDate(purchase.due_date)}</span>
                                )}
                                {purchase.days_outstanding && (
                                  <span className={`${
                                    purchase.days_outstanding > 30 ? 'text-red-600' : 'text-gray-400'
                                  }`}>
                                    ({purchase.days_outstanding} days outstanding)
                                  </span>
                                )}
                              </div>
                            </div>
                          </div>
                          <div className="text-right">
                            <p className="text-sm font-medium text-gray-900">
                              {formatCurrency(purchase.amount)}
                            </p>
                            <div className="flex items-center text-xs text-gray-500">
                              {purchase.type === 'order' ? (
                                <ShoppingCartIcon className="h-3 w-3 mr-1" />
                              ) : (
                                <DocumentTextIcon className="h-3 w-3 mr-1" />
                              )}
                              {purchase.type.toUpperCase()}
                            </div>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            </Card>
          </div>

          {/* Payment Schedule & Actions */}
          <div className="space-y-6">
            {/* Payment Schedule */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Payment Schedule</h3>
              </div>
              <div className="p-6">
                {loading ? (
                  <div className="animate-pulse space-y-3">
                    {Array.from({ length: 3 }).map((_, index) => (
                      <div key={index} className="flex justify-between">
                        <div className="h-3 bg-gray-300 rounded w-1/2"></div>
                        <div className="h-3 bg-gray-300 rounded w-1/4"></div>
                      </div>
                    ))}
                  </div>
                ) : (
                  <div className="space-y-3">
                    {paymentSchedule.map((payment, index) => (
                      <div key={index} className="flex justify-between items-center">
                        <div className="flex-1">
                          <p className="text-sm font-medium text-gray-900">{payment.supplier}</p>
                          <div className="flex items-center space-x-2">
                            <p className="text-xs text-gray-500">
                              {formatDate(payment.due_date)}
                            </p>
                            <span className={`px-1.5 py-0.5 text-xs font-medium rounded ${getPriorityColor(payment.priority)}`}>
                              {payment.priority.toUpperCase()}
                            </span>
                          </div>
                        </div>
                        <div className="text-right ml-2">
                          <p className="text-sm font-bold text-gray-900">
                            {formatCurrency(payment.amount)}
                          </p>
                          <p className={`text-xs ${
                            payment.days_until_due <= 3 ? 'text-red-600' : 
                            payment.days_until_due <= 7 ? 'text-yellow-600' : 'text-gray-500'
                          }`}>
                            {payment.days_until_due} days
                          </p>
                        </div>
                      </div>
                    ))}
                    <div className="pt-3 border-t border-gray-200">
                      <div className="flex justify-between">
                        <span className="font-bold text-gray-900">Total Due:</span>
                        <span className="font-bold text-indigo-600">
                          {formatCurrency(paymentSchedule.reduce((sum, payment) => sum + payment.amount, 0))}
                        </span>
                      </div>
                    </div>
                  </div>
                )}
              </div>
            </Card>

            {/* Quick Actions */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Quick Actions</h3>
              </div>
              <div className="p-6 space-y-3">
                <Button variant="outline" className="w-full justify-start">
                  <TruckIcon className="h-4 w-4 mr-2" />
                  Supplier Inquiry
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <ShoppingCartIcon className="h-4 w-4 mr-2" />
                  Create Purchase Order
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <DocumentTextIcon className="h-4 w-4 mr-2" />
                  Enter Invoice
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <BanknotesIcon className="h-4 w-4 mr-2" />
                  Process Payment
                </Button>
                <Button variant="outline" className="w-full justify-start">
                  <CheckCircleIcon className="h-4 w-4 mr-2" />
                  3-Way Matching
                </Button>
              </div>
            </Card>

            {/* Key Metrics */}
            {summary && (
              <Card>
                <div className="px-6 py-4 border-b border-gray-200">
                  <h3 className="text-lg font-medium text-gray-900">Key Metrics</h3>
                </div>
                <div className="p-6">
                  <dl className="space-y-3 text-sm">
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Avg Payment Days:</dt>
                      <dd className="font-medium text-gray-900">{summary.average_payment_days} days</dd>
                    </div>
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Pending Orders:</dt>
                      <dd className="font-medium text-gray-900">{summary.pending_orders}</dd>
                    </div>
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Invoice Matching Rate:</dt>
                      <dd className="font-medium text-gray-900">92.5%</dd>
                    </div>
                    <div className="flex justify-between">
                      <dt className="text-gray-500">Early Payment Discounts:</dt>
                      <dd className="font-medium text-green-600">$2,450</dd>
                    </div>
                  </dl>
                </div>
              </Card>
            )}
          </div>
        </div>

        {/* Alerts */}
        {summary && (summary.overdue_payments > 0 || summary.pending_approvals > 0) && (
          <div className="mt-8">
            <Card>
              <div className="p-6">
                <div className="rounded-md bg-yellow-50 p-4">
                  <div className="flex">
                    <div className="flex-shrink-0">
                      <ExclamationTriangleIcon className="h-5 w-5 text-yellow-400" />
                    </div>
                    <div className="ml-3">
                      <h3 className="text-sm font-medium text-yellow-800">
                        Action Required
                      </h3>
                      <div className="mt-2 text-sm text-yellow-700">
                        <ul className="list-disc pl-5 space-y-1">
                          {summary.overdue_payments > 0 && (
                            <li>
                              {formatCurrency(summary.overdue_payments)} in overdue payments
                            </li>
                          )}
                          {summary.pending_approvals > 0 && (
                            <li>{summary.pending_approvals} invoices awaiting approval</li>
                          )}
                          {summary.pending_orders > 0 && (
                            <li>{summary.pending_orders} purchase orders pending receipt</li>
                          )}
                          <li>Review early payment discount opportunities</li>
                        </ul>
                      </div>
                      <div className="mt-4">
                        <div className="flex space-x-2">
                          <Button size="sm" variant="outline">
                            Process Payments
                          </Button>
                          <Button size="sm" variant="outline">
                            Review Approvals
                          </Button>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </Card>
          </div>
        )}
      </main>
    </div>
  )
}