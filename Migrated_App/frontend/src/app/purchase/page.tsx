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
        // Fetch real data from API
        const summaryResponse = await fetch('http://localhost:8000/api/v1/pl/summary')
        if (summaryResponse.ok) {
          const summaryData = await summaryResponse.json()
          setSummary(summaryData)
        }
        
        const recentResponse = await fetch('http://localhost:8000/api/v1/pl/recent')
        if (recentResponse.ok) {
          const recentData = await recentResponse.json()
          setRecentPurchases(recentData.recent_purchases)
          setPaymentSchedule(recentData.payment_schedule)
        }
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
      />
      
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {loading ? (
          <div className="text-center py-12">
            <p className="text-gray-500">Loading purchase ledger data...</p>
          </div>
        ) : (
          <>
            {/* Summary Stats */}
            <div className="grid grid-cols-1 gap-5 sm:grid-cols-2 lg:grid-cols-4 mb-8">
              <StatsCard
                title="Total Suppliers"
                value={summary?.total_suppliers.toString() || '0'}
                subtitle={`${summary?.active_suppliers || 0} active`}
                icon={TruckIcon}
                color="indigo"
              />
              <StatsCard
                title="Total Payable"
                value={formatCurrency(summary?.total_payable || 0)}
                subtitle={`${formatCurrency(summary?.overdue_payments || 0)} overdue`}
                icon={CurrencyDollarIcon}
                color="red"
              />
              <StatsCard
                title="Pending Orders"
                value={summary?.pending_orders.toString() || '0'}
                subtitle={`${summary?.pending_invoices || 0} pending invoices`}
                icon={ShoppingCartIcon}
                color="yellow"
              />
              <StatsCard
                title="Avg Payment Days"
                value={summary?.average_payment_days.toString() || '0'}
                subtitle={`${summary?.pending_approvals || 0} pending approvals`}
                icon={ClockIcon}
                color="green"
              />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
              {/* Recent Purchases */}
              <Card className="h-full">
                <div className="px-6 py-4 border-b border-gray-200">
                  <h3 className="text-lg font-medium text-gray-900">
                    Recent Purchase Activity
                  </h3>
                  <p className="mt-1 text-sm text-gray-500">
                    Latest orders and invoices
                  </p>
                </div>
                <div className="px-6 py-4 space-y-4">
                  {recentPurchases.length > 0 ? (
                    recentPurchases.map((purchase) => (
                      <div key={purchase.id} className="flex items-start space-x-4">
                        <div className={`p-2 rounded-lg ${
                          purchase.type === 'order' ? 'bg-blue-50' : 'bg-purple-50'
                        }`}>
                          {purchase.type === 'order' ? (
                            <ShoppingCartIcon className={`h-5 w-5 ${
                              purchase.type === 'order' ? 'text-blue-600' : 'text-purple-600'
                            }`} />
                          ) : (
                            <DocumentTextIcon className="h-5 w-5 text-purple-600" />
                          )}
                        </div>
                        <div className="flex-1">
                          <div className="flex items-center justify-between">
                            <div>
                              <p className="text-sm font-medium text-gray-900">
                                {purchase.po_number || purchase.invoice_number}
                              </p>
                              <p className="text-sm text-gray-500">{purchase.supplier_name}</p>
                            </div>
                            <div className="text-right">
                              <p className="text-sm font-medium text-gray-900">
                                {formatCurrency(purchase.amount)}
                              </p>
                              <span className={`inline-flex items-center px-2 py-0.5 rounded text-xs font-medium ${
                                getStatusColor(purchase.status)
                              }`}>
                                {purchase.status.replace('_', ' ')}
                              </span>
                            </div>
                          </div>
                          <p className="text-xs text-gray-400 mt-1">
                            {purchase.order_date ? 
                              `Ordered: ${formatDate(purchase.order_date)}` : 
                              `Due: ${formatDate(purchase.due_date || '')}`
                            }
                          </p>
                        </div>
                      </div>
                    ))
                  ) : (
                    <p className="text-gray-500 text-sm">No recent purchases found</p>
                  )}
                </div>
              </Card>

              {/* Payment Schedule */}
              <Card className="h-full">
                <div className="px-6 py-4 border-b border-gray-200">
                  <div className="flex items-center justify-between">
                    <h3 className="text-lg font-medium text-gray-900">
                      Payment Schedule
                    </h3>
                    <BanknotesIcon className="h-5 w-5 text-gray-400" />
                  </div>
                  <p className="mt-1 text-sm text-gray-500">
                    Upcoming payments due
                  </p>
                </div>
                <div className="px-6 py-4">
                  <div className="space-y-4">
                    {paymentSchedule.length > 0 ? (
                      paymentSchedule.map((payment, index) => (
                        <div key={index} className="border-l-4 border-gray-200 pl-4 hover:border-indigo-500 transition-colors">
                          <div className="flex items-start justify-between">
                            <div>
                              <p className="text-sm font-medium text-gray-900">{payment.supplier}</p>
                              <div className="flex items-center mt-1 space-x-2">
                                <span className="text-sm text-gray-500">
                                  Due: {formatDate(payment.due_date)}
                                </span>
                                <span className={`inline-flex items-center px-2 py-0.5 rounded text-xs font-medium ${
                                  getPriorityColor(payment.priority)
                                }`}>
                                  {payment.priority} priority
                                </span>
                              </div>
                            </div>
                            <div className="text-right">
                              <p className="text-sm font-medium text-gray-900">
                                {formatCurrency(payment.amount)}
                              </p>
                              <p className={`text-xs ${
                                payment.days_until_due < 0 ? 'text-red-600' : 
                                payment.days_until_due <= 7 ? 'text-yellow-600' : 'text-gray-500'
                              }`}>
                                {payment.days_until_due < 0 ? 
                                  `${Math.abs(payment.days_until_due)} days overdue` :
                                  `${payment.days_until_due} days`
                                }
                              </p>
                            </div>
                          </div>
                          {payment.days_until_due < 0 && (
                            <div className="flex items-center mt-2">
                              <ExclamationTriangleIcon className="h-4 w-4 text-red-500 mr-1" />
                              <span className="text-xs text-red-600">Immediate action required</span>
                            </div>
                          )}
                        </div>
                      ))
                    ) : (
                      <p className="text-gray-500 text-sm">No upcoming payments</p>
                    )}
                  </div>
                  
                  <div className="mt-6 pt-6 border-t border-gray-200">
                    <div className="flex justify-between text-sm">
                      <span className="text-gray-500">Total due this month</span>
                      <span className="font-medium text-gray-900">
                        {formatCurrency(summary?.current_month_purchases || 0)}
                      </span>
                    </div>
                  </div>
                </div>
              </Card>
            </div>
          </>
        )}
      </main>
    </div>
  )
}